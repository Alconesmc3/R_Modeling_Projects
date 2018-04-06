import json
import sys

# wdl_file_test = "/home/math/Desktop/ucsc/WDL_tutorials_me_July2017/simpleVariantSelection.wdl"
# json_file_test = "/home/math/Desktop/ucsc/WDL_tutorials_me_July2017/simpleVariantSelection_inputs.json"


class FindWdlReplaceJsonPath(object):

    def __init__(self):
        #Declare my variables.
        self.wdl_list_var = []
        self.all_wdl_var_list = []
        self.json_list_var = []
        self.json_lower_list_var = []
        self.non_repeat_wdl_list = []
        self.lower_non_repeats_wdl = []
        self.var_to_path_dict = {}
        self.wdl_var_to_json_var_dict = {}
        self.max_var_replaced = 0


    '''This function will find all the WDL variables in a single line in format "${variable_name}" and
        strip the "${}" part of it.  It will return a list of all the variables in that line '''
    def FindAllWdlVarInLine(self, line_in_file):
        saved_string = ''
        start_saving = False
        list_of_saved_strings = []

        for char in line_in_file:
            if (char == '}') and (start_saving == True):
                list_of_saved_strings.append(saved_string[1:])
                saved_string = ''
                start_saving = False
            if start_saving is True:
                saved_string = saved_string + char
            if char == '$':
                start_saving = True
        # print(list_of_saved_strings)
        return list_of_saved_strings

    '''Make a new list of all WDL variables in the file by calling the FindAllWdlVarInLine function
        on every single line of the file. Name it self.all_wdl_var_list'''
    def NonRepeatWdlVarList(self, wdl_file):
        with open(wdl_file) as file:
            for line in file:
                for element in self.FindAllWdlVarInLine(line):
                    self.all_wdl_var_list.append(element)

        # print("Here is a list of all the WDL variables found in the WDL file in total, can find multiple variables per line: self.all_wdl_var_list")
        # print(self.all_wdl_var_list)

        #Create a new list that will have all the wdl variables without repeating variables.
        all_non_repeat_wdl = list(set(self.all_wdl_var_list))
        # print("Here is a list of all the WDL variables with no repeats allowed: all_non_repeat_wdl")
        # print(all_non_repeat_wdl)
        return all_non_repeat_wdl

    def JsonListAndDictToPath(self, json_file):
        # Open the JSON file and load it into: dataJson
        with open(json_file) as Jfile:
            dataJson = json.load(Jfile)

            # print the lines and the last index from list from line.split(), which will be the JSON variable
            # Create a dictionary *self.var_to_path_dict* with the keys as the Json variables similar to WDL variables and the values as
            # the respective file path that will replace the wdl variable.
            # Also create a list of all the variables that are found in the JSON file called *self.json_list_var*
            for line in dataJson:
                split_line = line.split('.')
                self.var_to_path_dict[split_line[-1]] = dataJson[line]
                self.json_list_var.append(split_line[-1])
        print(self.json_list_var)
        print(self.var_to_path_dict)

        return self.json_list_var, self.var_to_path_dict


    '''This function will find the longest common substring found inside the two strings *s1* and *s2*'''
    def longest_common_substring(self, s1, s2):
       m = [[0] * (1 + len(s2)) for i in xrange(1 + len(s1))]
       longest, x_longest = 0, 0
       for x in xrange(1, 1 + len(s1)):
           for y in xrange(1, 1 + len(s2)):
               if s1[x - 1] == s2[y - 1]:
                   m[x][y] = m[x - 1][y - 1] + 1
                   if m[x][y] > longest:
                       longest = m[x][y]
                       x_longest = x
               else:
                   m[x][y] = 0
       return s1[x_longest - longest: x_longest]

    '''This function will use the previous function *longest_common_substring* in order to compare a list of strings *list_str*
        with a single string *str_compare*. It will return the string in the list that has the longest common substring 
        with the string to be compared *str_compare* '''
    def list_longest_common_substring(self, list_of_str, str_compare):
        '''This function will compare the string *str_compare* to every string in the list *list_str* and will
           return the string in that list that has the largest common substring(LCS) with *str_compare*, only if
           the lenght of the LCS is greater than n >= 4.'''
        similar_string = ''
        len_common_substring = []
        for var in list_of_str:
            len_common_substring.append(len(self.longest_common_substring(var, str_compare)))
            if max(len_common_substring) >= 4:
                lcs_index = len_common_substring.index(max(len_common_substring))
                similar_string = list_of_str[lcs_index]
        return similar_string

    def CompareVarCreateDict(self, all_non_repeat_wdl):
        '''This will compare the JSON variables with the WDL variables, if they match, they will be placed in the dictionary
           *self.wdl_var_to_json_var_dict* where the key is the variable in WDL with "${}" in it and the value is the similar variable
           in the JSON file'''
        for jvar in self.json_list_var:
            # print('\n' + 'This is the current JSON variable in the loop: ' + jvar)
            for wvar in all_non_repeat_wdl:
                # print('\n' + "We will compare with this variable with: " + wvar)
                if jvar == wvar:
                    # print("Wdl variable and Json variables are Exact Matches(EM)")
                    self.wdl_var_to_json_var_dict["${" + wvar + "}"] = jvar
                    continue

                elif wvar.lower() == jvar.lower():
                    # print("Wdl lowercase variable is equal to the Json lowercase Variable (ELM)")
                    self.wdl_var_to_json_var_dict["${" + wvar + "}"] = jvar
                    continue
                elif wvar.lower() in jvar.lower():
                    # print("Wdl lowercase variable is in the Json lowercase variable. In Other Lowercase Match (IOLM)")
                    self.wdl_var_to_json_var_dict["${" + wvar + "}"] = jvar
                    continue
                elif jvar.lower() in wvar.lower():
                    # print("Json lowercase variable is in the Wdl lowercase variable. In Other Lowercase Match (IOLM)")
                    self.wdl_var_to_json_var_dict["${" + wvar + "}"] = jvar
                    continue
                else:
                    # print("These two variables don't match in any way")
                    continue
        # print(self.wdl_var_to_json_var_dict)

        return self.wdl_var_to_json_var_dict

    def RemoveMatchedVarList(self, all_non_repeat_wdl):
        '''Create a new dictionary that will lowercase the variables in the previous dictionary and strip the "${}" from keys.'''
        wdl_to_json_dict_stripped = {key[2:-1]: value for key, value in self.wdl_var_to_json_var_dict.items()}
        # print(wdl_to_json_dict_stripped)
        # Once all the JSON variables are matched and put in dictionary, remove these variables from their lists to check again.

        for key, value in wdl_to_json_dict_stripped.items():
            if key in all_non_repeat_wdl:
                all_non_repeat_wdl.remove(key)
            if value in self.json_list_var:
                self.json_list_var.remove(value)
        # print(all_non_repeat_wdl)
        # print(self.json_list_var)
        return all_non_repeat_wdl, self.json_list_var

    def MatchSimilarVarLCSFourAndUP(self, all_non_repeat_wdl):
        '''This will use *self.json_list_var* and the function *list_longest_common_substring* to attempt to match more WDL variables
            with JSON variables and add them to the dictionary *self.wdl_var_to_json_var_dict* in order for the WDL variables to be
            recognized after and be replaced. It will return a dictionary *self.wdl_var_to_json_var_dict* and a list of variables
             found in the JSON file that were not able to be matched *jvar_not_matched*'''
        jvar_not_matched = []
        for jvar in self.json_list_var:
            # print(list_longest_common_substring(all_non_repeat_wdl, jvar))
            if (self.list_longest_common_substring(all_non_repeat_wdl, jvar)) != '':
                similar_wdl_var = (self.list_longest_common_substring(all_non_repeat_wdl, jvar))
                self.wdl_var_to_json_var_dict["${" + similar_wdl_var + "}"] = jvar
            else:
                jvar_not_matched.append(jvar)
                print("There is no LCS greater than or equal to 4, no match found")

        '''This has matched all the test case encountered so far, those unmatched don't have a LCS of n = 4 or greater.'''
        print(self.wdl_var_to_json_var_dict)
        print(len(self.wdl_var_to_json_var_dict))

        print("These are the JSON variables that were not able to be matched")
        print(jvar_not_matched)

        return self.wdl_var_to_json_var_dict, jvar_not_matched

    '''This function will create an empty copy file of the WDL file, with "_copy.wdl" at the end of 
        the filepath name.  Then it will read the WDL file and write the lines onto that new copy file, 
        if they contain a WDL variable, it will replace it by the respective filepath'''

    def NewFileWithReplacements(self, wdl_file):
        '''Create a new file that is a copy of the WDL file with all variables replaced.'''
        copy_wdl_file = wdl_file.replace(".wdl", "_copy.wdl")
        with open(wdl_file, 'r') as rf:
            with open(copy_wdl_file, 'w') as wf:
                for line in rf:
                    line_already_printed = False
                    for key, value in self.wdl_var_to_json_var_dict.items():
                        # print(key)
                        if key in line:
                            print(line.replace(key, self.var_to_path_dict[value]))
                            wf.write(line.replace(key, self.var_to_path_dict[value]))
                            line_already_printed = True
                            break
                    if line_already_printed == False:
                        wf.write(line)
                        print(line)
        return wf

def main(argv):

    wdl_file_test = "/home/math/Desktop/ucsc/WDL_tutorials_me_July2017/jointCallingGenotypes.wdl"
    json_file_test = "/home/math/Desktop/ucsc/WDL_tutorials_me_July2017/jointCallingGenotypes_inputs.json"

    i = FindWdlReplaceJsonPath()
    wdl_var_no_repeats = i.NonRepeatWdlVarList(wdl_file_test)
    i.JsonListAndDictToPath(json_file_test)
    i.CompareVarCreateDict(wdl_var_no_repeats)
    i.RemoveMatchedVarList(wdl_var_no_repeats)
    i.MatchSimilarVarLCSFourAndUP(wdl_var_no_repeats)
    i.NewFileWithReplacements(wdl_file_test)

if __name__ == "__main__":
    main(sys.argv)