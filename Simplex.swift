//
//  File.swift
//  DietOptimization
//
//  Created by WuYulong on 25/01/2017.
//  Copyright © 2017 HD90+. All rights reserved.
//
//import Foundation



class LinearProgram
{
    var numberOfSubjects :Int
    var numberOfVariables :Int
    var numberOfLessEqualto :Int
    var numberOfEqualto :Int
    var numberOfLargerEqualto :Int
    var error :Int
    var basic = [Int]()
    var nonbasic = [Int]()
    var subjectsMatrix = [[Double]]()
    var optimalResult :Double
    
    
    
    init(optimalResult :Double, numberOfSubjects :Int, numberOfVariables :Int, numberOfLessEqualto :Int,numberOfEqualto :Int, numberOfLargerEqualto :Int, subjectsMatrix :[[Double]], x :[Double])
    {
        var _value :Double
        self.error=0
        self.optimalResult=optimalResult
        self.numberOfSubjects=numberOfSubjects
        self.numberOfVariables=numberOfVariables
        self.numberOfLessEqualto=numberOfLessEqualto
        self.numberOfEqualto=numberOfEqualto
        self.numberOfLargerEqualto=numberOfLargerEqualto
        
        if (numberOfSubjects != numberOfLessEqualto + numberOfEqualto + numberOfLargerEqualto)
        {
            self.error=1
        }
        
        self.subjectsMatrix = [[Double]](repeating: [Double](repeating :0.0, count: numberOfVariables),count: numberOfSubjects + 2)
         for  i in 0 ..< numberOfSubjects+2
         {
            self.subjectsMatrix[i] = [Double](repeating :0.0, count: numberOfVariables + numberOfSubjects + numberOfLargerEqualto + 1)
         }
        
        
        self.basic = [Int](repeating :0 , count :numberOfSubjects + 2)
         self.nonbasic = [Int](repeating :0, count :numberOfVariables + numberOfLargerEqualto + 1)
        
        for i in 0...numberOfSubjects + 1
        {
            for j in 0...numberOfVariables + numberOfSubjects + numberOfLargerEqualto
            {
                self.subjectsMatrix[i][j]=0.0
            }
        }
 
        for j in 0 ... numberOfVariables + numberOfLargerEqualto
        {
            nonbasic[j]=j
        }
        for i in 1...numberOfSubjects
        {
            basic[i] = numberOfVariables + numberOfLargerEqualto + i
        }
        
        
        for i in numberOfSubjects - numberOfLargerEqualto + 1...numberOfSubjects

        {
            self.subjectsMatrix[i][i - numberOfSubjects + numberOfLargerEqualto + numberOfVariables] = -1
            self.subjectsMatrix[numberOfSubjects + 1][i - numberOfSubjects + numberOfLargerEqualto + numberOfVariables] = -1
            
        }
        for i in 1...numberOfSubjects
        {
            for j in 1...numberOfVariables
            {
                _value = subjectsMatrix[i-1][j-1]
                self.subjectsMatrix[i][j]=_value
            }
            _value=subjectsMatrix[i-1][numberOfVariables]
            if (_value < 0)
            {
                error = 1
            }
            self.subjectsMatrix[i][0]=_value
        }
        for j in 1...numberOfVariables
        {
            _value = Double(x[j-1])
            self.subjectsMatrix[0][j] = _value * optimalResult
        }
        for j in 1...numberOfVariables
        {
            _value=0
            for i in numberOfLessEqualto+1 ... numberOfSubjects
            {
                _value += self.subjectsMatrix[i][j]
                /////////////////////////////
                self.subjectsMatrix[numberOfSubjects + 1 ][j] = _value
            }
        }
        
    }
    
    func enter (objRow :Int) -> Int
    {
        var col = 0
        for j in 1 ... self.numberOfVariables + self.numberOfLargerEqualto
        {
            if( self.nonbasic[j] <= self.numberOfVariables + self.numberOfLessEqualto + self.numberOfLargerEqualto && self.subjectsMatrix[objRow][j]>10e-8)
            {
                col = j
                break
            }
            
        }
        return col
    }
    func leave (col :Int) ->Int
    {
        var temp = -1.0
        var row = 0
        for i in 1...self.numberOfSubjects
        {
            var _val = self.subjectsMatrix[i][col]
            if(_val > 10e-8)
            {
                _val=self.subjectsMatrix[i][0] / _val
                if(_val < temp || temp == -1)
                {
                    row = i
                    temp = _val
                }
            }
        }
        return row
    }
    func swapBasic(row :Int, col :Int)
    {
        let temp = self.basic[row]
        self.basic[row] = self.nonbasic[col]
        self.nonbasic[col] = temp
    }
    func pivot(row :Int, col :Int)
    {
        for j in 0 ... self.numberOfVariables + self.numberOfLargerEqualto
        {
            if (j != col)
            {
                self.subjectsMatrix[row][j] = self.subjectsMatrix[row][j] / self.subjectsMatrix[row][col]
            }
        }
        self.subjectsMatrix[row][col] = 1.0 / self.subjectsMatrix[row][col]
        for i in 0...self.numberOfSubjects + 1
        {
            if (i != row)
            {
                for j in 0...self.numberOfVariables + numberOfLargerEqualto
                {
                    if( j != col)
                    {
                        self.subjectsMatrix[i][j] = self.subjectsMatrix[i][j] - self.subjectsMatrix[i][col] * self.subjectsMatrix[row][j]
                        if(abs(self.subjectsMatrix[i][j]) < 10e-8)
                        {
                            self.subjectsMatrix[i][j] = 0
                        }
                    }
                }
                self.subjectsMatrix[i][col] = -self.subjectsMatrix[i][col] * self.subjectsMatrix[row][col]
            }
        }
        swapBasic(row: row, col: col)
    }
    func simplex(objRow :Int) -> Int {
        var row = 0
        while (true)
        {
            let col = enter(objRow: objRow)
            if (col > 0)
            {
                row = leave(col: col)
            }
            else
            {
                return 0
            }
            if(row > 0)
            {
                pivot(row: row, col: col)
            }
            else
            {
                return 2
            }
        }
    }
    func phase1() -> Int
    {
        self.error = simplex(objRow: self.numberOfSubjects + 1)
        if (self.error > 0)
        {
            return self.error
        }
        for i in 1 ... self.numberOfSubjects
        {
            if(self.basic[i] > self.numberOfVariables + self.numberOfLessEqualto + self.numberOfLargerEqualto)
            {
                if(self.subjectsMatrix[i][0] > 10e-8)
                {
                    return 3
                }
                for j in 1...self.numberOfVariables
                {
                    if(abs(self.subjectsMatrix[i][j]) >= 10e-8)
                    {
                        pivot(row: i, col: j)
                        break
                    }
                }
            }
        }
        return 0
    }
    
    func phase2() -> Int {
        return simplex(objRow: 0)
    }
    func compute() -> Int
    {
        if(self.error > 0)
        {
            return self.error
        }
        if(self.numberOfSubjects != self.numberOfLessEqualto)
        {
            self.error=phase1()
            if(self.error>0)
            {
                return self.error
            }
        }
        return phase2()
    }
    func solve()
    {
        error = compute()
        switch error
        {
        case 0:
            output()
            break
        case 1:
            print("parameters input error!")
        case 2:
            print("no boundaries solutions!")
        case 3:
            print("no solutions!")
        default:
            break
        }
    }
    func output()
    {
        var basicCp = [Int](repeating :0, count :numberOfVariables + 1)
        for i in 0...numberOfVariables
        {
            basicCp[i]=0
        }
        for i in 1...numberOfSubjects
        {
            if(basic[i] >= 1 && basic[i] <= numberOfVariables)
            {
                basicCp[basic[i]] = i
            }
        }
        for i in 1...numberOfVariables
        {
            //print("food \(i) =")
            if(basicCp[i] != 0)
            {
                print("food \(i) = \(subjectsMatrix[basicCp[i]][0])")
                            }
            else
            {
                print("food \(i) = 0")
            }
        }
        print("Optimal Results: ")
        print(-optimalResult * subjectsMatrix[0][0], separator: " ", terminator: " ")
    }
    func foodResult() -> [Double]
    {
        var temp = [Int](repeating :0, count :numberOfVariables + 1)
        var foodResultArr = [Double](repeating :0, count :numberOfVariables + 1)
        for i in 0...numberOfVariables
            {
                temp[i]=0
            }
            for i in 1...numberOfSubjects
            {
                if(basic[i] >= 1 && basic[i] <= numberOfVariables)
                {
                    temp[basic[i]] = i
                }
            }
            for i in 1...numberOfVariables
            {
                if(temp[i] != 0)
                {
                    //print("food \(i) = \(subjectsMatrix[temp[i]][0])")
                    foodResultArr[i-1] = subjectsMatrix[temp[i]][0]
                }
                else
                {
                    foodResultArr[i-1] = 0
                }
            }
      
        return foodResultArr
    }
    
    func resultPrompt() ->Int
    {
        error = compute()
        var prompt = 0
        switch error
        {
        case 0:
            prompt = 0
        case 1:
            prompt = 1
        case 2:
           prompt = 2
        case 3:
            prompt = 3
        default:
            break
        }
        return prompt
    }
}
//func Test(){
//let a = [[-1.0,1.0,1.0,20.0],[1.0,-3.0,1.0,30.0],[1.0,0.0,0.0,40.0],[1.0,0.0,0.0,0.0],[0.0,1.0,0.0,0.0],[0.0,0.0,1.0,0.0]]//subjects coeficients matrix
//let x = [1,2,3];//taget function coeficients
//let lp = LinearProgram(optimalResult: 1,numberOfSubjects: 6,numberOfVariables: 3,numberOfLessEqualto: 3,numberOfEqualto: 0,numberOfLargerEqualto: 3,subjectsMatrix: a, x: x)
//
//print (lp.solve())
//}


